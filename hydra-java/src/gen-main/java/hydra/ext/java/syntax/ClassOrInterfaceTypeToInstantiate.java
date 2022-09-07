package hydra.ext.java.syntax;

public class ClassOrInterfaceTypeToInstantiate {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ClassOrInterfaceTypeToInstantiate");
  
  public final java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers;
  
  public final java.util.Optional<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments;
  
  public ClassOrInterfaceTypeToInstantiate (java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers, java.util.Optional<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
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
  
  public ClassOrInterfaceTypeToInstantiate withIdentifiers(java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers) {
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
  
  public ClassOrInterfaceTypeToInstantiate withTypeArguments(java.util.Optional<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
}