// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_ReferenceType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_ReferenceType");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE_TYPE = new hydra.core.Name("referenceType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.ReferenceType referenceType;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public MethodReference_ReferenceType (hydra.ext.java.syntax.ReferenceType referenceType, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((referenceType));
    java.util.Objects.requireNonNull((typeArguments));
    java.util.Objects.requireNonNull((identifier));
    this.referenceType = referenceType;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_ReferenceType)) {
      return false;
    }
    MethodReference_ReferenceType o = (MethodReference_ReferenceType) (other);
    return referenceType.equals(o.referenceType) && typeArguments.equals(o.typeArguments) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * referenceType.hashCode() + 3 * typeArguments.hashCode() + 5 * identifier.hashCode();
  }
  
  public MethodReference_ReferenceType withReferenceType(hydra.ext.java.syntax.ReferenceType referenceType) {
    java.util.Objects.requireNonNull((referenceType));
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
  
  public MethodReference_ReferenceType withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
  
  public MethodReference_ReferenceType withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
}