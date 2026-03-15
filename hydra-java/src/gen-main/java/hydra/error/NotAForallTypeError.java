// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A type that is not a forall type when type arguments are being applied
 */
public class NotAForallTypeError implements Serializable, Comparable<NotAForallTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.NotAForallTypeError");
  
  public static final hydra.core.Name TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  /**
   * The actual type encountered
   */
  public final hydra.core.Type type;
  
  /**
   * The type arguments that were being applied
   */
  public final hydra.util.ConsList<hydra.core.Type> typeArguments;
  
  public NotAForallTypeError (hydra.core.Type type, hydra.util.ConsList<hydra.core.Type> typeArguments) {
    this.type = type;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotAForallTypeError)) {
      return false;
    }
    NotAForallTypeError o = (NotAForallTypeError) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(typeArguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NotAForallTypeError other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
  }
  
  public NotAForallTypeError withType(hydra.core.Type type) {
    return new NotAForallTypeError(type, typeArguments);
  }
  
  public NotAForallTypeError withTypeArguments(hydra.util.ConsList<hydra.core.Type> typeArguments) {
    return new NotAForallTypeError(type, typeArguments);
  }
}
