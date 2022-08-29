package hydra.ext.java.syntax;

public class MethodReference_ReferenceType {
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
    MethodReference_ReferenceType o = (MethodReference_ReferenceType) (other);
    return referenceType.equals(o.referenceType) && typeArguments.equals(o.typeArguments) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * referenceType.hashCode() + 3 * typeArguments.hashCode() + 5 * identifier.hashCode();
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