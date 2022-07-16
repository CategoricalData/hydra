package hydra.ext.java.syntax;

public class MethodReference_ReferenceType {
  public final ReferenceType referenceType;
  
  public final java.util.List<TypeArgument> typeArguments;
  
  public final Identifier identifier;
  
  public MethodReference_ReferenceType (ReferenceType referenceType, java.util.List<TypeArgument> typeArguments, Identifier identifier) {
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
  
  public MethodReference_ReferenceType withReferenceType(ReferenceType referenceType) {
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
  
  public MethodReference_ReferenceType withTypeArguments(java.util.List<TypeArgument> typeArguments) {
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
  
  public MethodReference_ReferenceType withIdentifier(Identifier identifier) {
    return new MethodReference_ReferenceType(referenceType, typeArguments, identifier);
  }
}