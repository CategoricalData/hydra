// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InterfaceMethodDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfaceMethodDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_TYPE = new hydra.core.Name("returnType");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final Boolean new_;
  
  public final hydra.ext.csharp.syntax.ReturnType returnType;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind;
  
  public final hydra.ext.csharp.syntax.InterfaceMethodHeader header;
  
  public InterfaceMethodDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, Boolean new_, hydra.ext.csharp.syntax.ReturnType returnType, hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind, hydra.ext.csharp.syntax.InterfaceMethodHeader header) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((new_));
    java.util.Objects.requireNonNull((returnType));
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((header));
    this.attributes = attributes;
    this.new_ = new_;
    this.returnType = returnType;
    this.refKind = refKind;
    this.header = header;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceMethodDeclaration)) {
      return false;
    }
    InterfaceMethodDeclaration o = (InterfaceMethodDeclaration) (other);
    return attributes.equals(o.attributes) && new_.equals(o.new_) && returnType.equals(o.returnType) && refKind.equals(o.refKind) && header.equals(o.header);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * new_.hashCode() + 5 * returnType.hashCode() + 7 * refKind.hashCode() + 11 * header.hashCode();
  }
  
  public InterfaceMethodDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new InterfaceMethodDeclaration(attributes, new_, returnType, refKind, header);
  }
  
  public InterfaceMethodDeclaration withNew(Boolean new_) {
    java.util.Objects.requireNonNull((new_));
    return new InterfaceMethodDeclaration(attributes, new_, returnType, refKind, header);
  }
  
  public InterfaceMethodDeclaration withReturnType(hydra.ext.csharp.syntax.ReturnType returnType) {
    java.util.Objects.requireNonNull((returnType));
    return new InterfaceMethodDeclaration(attributes, new_, returnType, refKind, header);
  }
  
  public InterfaceMethodDeclaration withRefKind(hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new InterfaceMethodDeclaration(attributes, new_, returnType, refKind, header);
  }
  
  public InterfaceMethodDeclaration withHeader(hydra.ext.csharp.syntax.InterfaceMethodHeader header) {
    java.util.Objects.requireNonNull((header));
    return new InterfaceMethodDeclaration(attributes, new_, returnType, refKind, header);
  }
}