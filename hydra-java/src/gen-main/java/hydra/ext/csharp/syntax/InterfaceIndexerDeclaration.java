// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InterfaceIndexerDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfaceIndexerDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_REF_KIND = new hydra.core.Name("refKind");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_ACCESSORS = new hydra.core.Name("accessors");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final Boolean new_;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.FormalParameterList parameters;
  
  public final hydra.ext.csharp.syntax.InterfaceAccessors accessors;
  
  public InterfaceIndexerDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, Boolean new_, hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.FormalParameterList parameters, hydra.ext.csharp.syntax.InterfaceAccessors accessors) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((new_));
    java.util.Objects.requireNonNull((refKind));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((accessors));
    this.attributes = attributes;
    this.new_ = new_;
    this.refKind = refKind;
    this.type = type;
    this.parameters = parameters;
    this.accessors = accessors;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceIndexerDeclaration)) {
      return false;
    }
    InterfaceIndexerDeclaration o = (InterfaceIndexerDeclaration) (other);
    return attributes.equals(o.attributes) && new_.equals(o.new_) && refKind.equals(o.refKind) && type.equals(o.type) && parameters.equals(o.parameters) && accessors.equals(o.accessors);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * new_.hashCode() + 5 * refKind.hashCode() + 7 * type.hashCode() + 11 * parameters.hashCode() + 13 * accessors.hashCode();
  }
  
  public InterfaceIndexerDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new InterfaceIndexerDeclaration(attributes, new_, refKind, type, parameters, accessors);
  }
  
  public InterfaceIndexerDeclaration withNew(Boolean new_) {
    java.util.Objects.requireNonNull((new_));
    return new InterfaceIndexerDeclaration(attributes, new_, refKind, type, parameters, accessors);
  }
  
  public InterfaceIndexerDeclaration withRefKind(hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> refKind) {
    java.util.Objects.requireNonNull((refKind));
    return new InterfaceIndexerDeclaration(attributes, new_, refKind, type, parameters, accessors);
  }
  
  public InterfaceIndexerDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new InterfaceIndexerDeclaration(attributes, new_, refKind, type, parameters, accessors);
  }
  
  public InterfaceIndexerDeclaration withParameters(hydra.ext.csharp.syntax.FormalParameterList parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new InterfaceIndexerDeclaration(attributes, new_, refKind, type, parameters, accessors);
  }
  
  public InterfaceIndexerDeclaration withAccessors(hydra.ext.csharp.syntax.InterfaceAccessors accessors) {
    java.util.Objects.requireNonNull((accessors));
    return new InterfaceIndexerDeclaration(attributes, new_, refKind, type, parameters, accessors);
  }
}