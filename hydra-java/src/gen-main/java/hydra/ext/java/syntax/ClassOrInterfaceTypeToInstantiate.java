// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ClassOrInterfaceTypeToInstantiate implements Serializable, Comparable<ClassOrInterfaceTypeToInstantiate> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIERS = new hydra.core.Name("identifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments;
  
  public ClassOrInterfaceTypeToInstantiate (java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers, hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
    this.identifiers = identifiers;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassOrInterfaceTypeToInstantiate)) {
      return false;
    }
    ClassOrInterfaceTypeToInstantiate o = (ClassOrInterfaceTypeToInstantiate) other;
    return java.util.Objects.equals(
      this.identifiers,
      o.identifiers) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(identifiers) + 3 * java.util.Objects.hashCode(typeArguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassOrInterfaceTypeToInstantiate other) {
    int cmp = 0;
    cmp = Integer.compare(
      identifiers.hashCode(),
      other.identifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
  }
  
  public ClassOrInterfaceTypeToInstantiate withIdentifiers(java.util.List<hydra.ext.java.syntax.AnnotatedIdentifier> identifiers) {
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
  
  public ClassOrInterfaceTypeToInstantiate withTypeArguments(hydra.util.Maybe<hydra.ext.java.syntax.TypeArgumentsOrDiamond> typeArguments) {
    return new ClassOrInterfaceTypeToInstantiate(identifiers, typeArguments);
  }
}
