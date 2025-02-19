// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ClassOrInterfaceTypeToInstantiate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIERS = new hydra.core.Name("identifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments;
  
  public ClassOrInterfaceTypeToInstantiate (java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers, hydra.util.Opt<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
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
  
  public ClassOrInterfaceTypeToInstantiate withIdentifiers(java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers) {
    java.util.Objects.requireNonNull((identifiers));
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
  
  public ClassOrInterfaceTypeToInstantiate withTypeArguments(hydra.util.Opt<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
}