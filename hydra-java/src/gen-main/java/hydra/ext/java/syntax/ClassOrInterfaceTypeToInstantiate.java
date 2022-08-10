package hydra.ext.java.syntax;

public class ClassOrInterfaceTypeToInstantiate {
  public final java.util.List<AnnotatedIdentifier> identifiers;
  
  public final java.util.Optional<TypeArgumentsOrDiamond> typeArguments;
  
  public ClassOrInterfaceTypeToInstantiate (java.util.List<AnnotatedIdentifier> identifiers, java.util.Optional<TypeArgumentsOrDiamond> typeArguments) {
    this.identifiers = identifiers;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassOrInterfaceTypeToInstantiate)) {
      return false;
    }
    ClassOrInterfaceTypeToInstantiate o = (ClassOrInterfaceTypeToInstantiate) (other);
    return identifiers.equals(o.identifiers) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifiers.hashCode() + 3 * typeArguments.hashCode();
  }
  
  public ClassOrInterfaceTypeToInstantiate withIdentifiers(java.util.List<AnnotatedIdentifier> identifiers) {
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
  
  public ClassOrInterfaceTypeToInstantiate withTypeArguments(java.util.Optional<TypeArgumentsOrDiamond> typeArguments) {
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
}