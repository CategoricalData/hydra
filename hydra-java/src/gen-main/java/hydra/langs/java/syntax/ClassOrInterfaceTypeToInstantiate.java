// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ClassOrInterfaceTypeToInstantiate implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ClassOrInterfaceTypeToInstantiate");
  
  public final java.util.List<hydra.langs.java.syntax.AnnotatedIdentifier> identifiers;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.TypeArgumentsOrDiamond> typeArguments;
  
  public ClassOrInterfaceTypeToInstantiate (java.util.List<hydra.langs.java.syntax.AnnotatedIdentifier> identifiers, hydra.util.Opt<hydra.langs.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
    java.util.Objects.requireNonNull((identifiers));
    java.util.Objects.requireNonNull((typeArguments));
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
  
  public ClassOrInterfaceTypeToInstantiate withIdentifiers(java.util.List<hydra.langs.java.syntax.AnnotatedIdentifier> identifiers) {
    java.util.Objects.requireNonNull((identifiers));
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
  
  public ClassOrInterfaceTypeToInstantiate withTypeArguments(hydra.util.Opt<hydra.langs.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
}