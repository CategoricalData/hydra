// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TypedAssignment implements Serializable, Comparable<TypedAssignment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TypedAssignment");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.SingleTarget lhs;
  
  public final hydra.ext.python.syntax.Expression type;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.AnnotatedRhs> rhs;
  
  public TypedAssignment (hydra.ext.python.syntax.SingleTarget lhs, hydra.ext.python.syntax.Expression type, hydra.util.Maybe<hydra.ext.python.syntax.AnnotatedRhs> rhs) {
    this.lhs = lhs;
    this.type = type;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedAssignment)) {
      return false;
    }
    TypedAssignment o = (TypedAssignment) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypedAssignment other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      rhs.hashCode(),
      other.rhs.hashCode());
  }
  
  public TypedAssignment withLhs(hydra.ext.python.syntax.SingleTarget lhs) {
    return new TypedAssignment(lhs, type, rhs);
  }
  
  public TypedAssignment withType(hydra.ext.python.syntax.Expression type) {
    return new TypedAssignment(lhs, type, rhs);
  }
  
  public TypedAssignment withRhs(hydra.util.Maybe<hydra.ext.python.syntax.AnnotatedRhs> rhs) {
    return new TypedAssignment(lhs, type, rhs);
  }
}
