// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InterfacePropertyDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfacePropertyDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ACCESSORS = new hydra.core.Name("accessors");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final Boolean new_;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.ext.csharp.syntax.InterfaceAccessors accessors;
  
  public InterfacePropertyDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, Boolean new_, hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.Identifier name, hydra.ext.csharp.syntax.InterfaceAccessors accessors) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((new_));
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((accessors));
    this.attributes = attributes;
    this.new_ = new_;
    this.refKind = refKind;
    this.type = type;
    this.name = name;
    this.accessors = accessors;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfacePropertyDeclaration)) {
      return false;
    }
    InterfacePropertyDeclaration o = (InterfacePropertyDeclaration) (other);
    return attributes.equals(o.attributes) && new_.equals(o.new_) && refKind.equals(o.refKind) && type.equals(o.type) && name.equals(o.name) && accessors.equals(o.accessors);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * new_.hashCode() + 5 * refKind.hashCode() + 7 * type.hashCode() + 11 * name.hashCode() + 13 * accessors.hashCode();
  }
  
  public InterfacePropertyDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new InterfacePropertyDeclaration(attributes, new_, refKind, type, name, accessors);
  }
  
  public InterfacePropertyDeclaration withNew(Boolean new_) {
    java.util.Objects.requireNonNull((new_));
    return new InterfacePropertyDeclaration(attributes, new_, refKind, type, name, accessors);
  }
  
  public InterfacePropertyDeclaration withRefKind(hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new InterfacePropertyDeclaration(attributes, new_, refKind, type, name, accessors);
  }
  
  public InterfacePropertyDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new InterfacePropertyDeclaration(attributes, new_, refKind, type, name, accessors);
  }
  
  public InterfacePropertyDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new InterfacePropertyDeclaration(attributes, new_, refKind, type, name, accessors);
  }
  
  public InterfacePropertyDeclaration withAccessors(hydra.ext.csharp.syntax.InterfaceAccessors accessors) {
    java.util.Objects.requireNonNull((accessors));
    return new InterfacePropertyDeclaration(attributes, new_, refKind, type, name, accessors);
  }
}