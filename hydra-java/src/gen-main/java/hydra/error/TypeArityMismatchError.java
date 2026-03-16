// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A type constructor applied to the wrong number of type arguments
 */
public class TypeArityMismatchError implements Serializable, Comparable<TypeArityMismatchError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.TypeArityMismatchError");
  
  public static final hydra.core.Name TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name EXPECTED_ARITY = new hydra.core.Name("expectedArity");
  
  public static final hydra.core.Name ACTUAL_ARITY = new hydra.core.Name("actualArity");
  
  public static final hydra.core.Name TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  /**
   * The type being checked
   */
  public final hydra.core.Type type;
  
  /**
   * The expected number of type arguments
   */
  public final Integer expectedArity;
  
  /**
   * The actual number of type arguments provided
   */
  public final Integer actualArity;
  
  /**
   * The type arguments that were provided
   */
  public final hydra.util.ConsList<hydra.core.Type> typeArguments;
  
  public TypeArityMismatchError (hydra.core.Type type, Integer expectedArity, Integer actualArity, hydra.util.ConsList<hydra.core.Type> typeArguments) {
    this.type = type;
    this.expectedArity = expectedArity;
    this.actualArity = actualArity;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeArityMismatchError)) {
      return false;
    }
    TypeArityMismatchError o = (TypeArityMismatchError) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.expectedArity,
      o.expectedArity) && java.util.Objects.equals(
      this.actualArity,
      o.actualArity) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(expectedArity) + 5 * java.util.Objects.hashCode(actualArity) + 7 * java.util.Objects.hashCode(typeArguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeArityMismatchError other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) expectedArity).compareTo(other.expectedArity);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) actualArity).compareTo(other.actualArity);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) typeArguments).compareTo(other.typeArguments);
  }
  
  public TypeArityMismatchError withType(hydra.core.Type type) {
    return new TypeArityMismatchError(type, expectedArity, actualArity, typeArguments);
  }
  
  public TypeArityMismatchError withExpectedArity(Integer expectedArity) {
    return new TypeArityMismatchError(type, expectedArity, actualArity, typeArguments);
  }
  
  public TypeArityMismatchError withActualArity(Integer actualArity) {
    return new TypeArityMismatchError(type, expectedArity, actualArity, typeArguments);
  }
  
  public TypeArityMismatchError withTypeArguments(hydra.util.ConsList<hydra.core.Type> typeArguments) {
    return new TypeArityMismatchError(type, expectedArity, actualArity, typeArguments);
  }
}
