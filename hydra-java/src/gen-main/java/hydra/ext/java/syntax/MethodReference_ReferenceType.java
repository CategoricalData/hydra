// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_ReferenceType implements Serializable, Comparable<MethodReference_ReferenceType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_ReferenceType");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE_TYPE = new hydra.core.Name("referenceType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.ReferenceType referenceType;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public MethodReference_ReferenceType (hydra.ext.java.syntax.ReferenceType referenceType, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier) {
    this.referenceType = referenceType;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_ReferenceType)) {
      return false;
    }
    MethodReference_ReferenceType o = (MethodReference_ReferenceType) other;
    return java.util.Objects.equals(
      this.referenceType,
      o.referenceType) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(referenceType) + 3 * java.util.Objects.hashCode(typeArguments) + 5 * java.util.Objects.hashCode(identifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodReference_ReferenceType other) {
    int cmp = 0;
    cmp = ((Comparable) referenceType).compareTo(other.referenceType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) identifier).compareTo(other.identifier);
  }
  
  public MethodReference_ReferenceType withReferenceType(hydra.ext.java.syntax.ReferenceType referenceType) {
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
  
  public MethodReference_ReferenceType withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
  
  public MethodReference_ReferenceType withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
}
