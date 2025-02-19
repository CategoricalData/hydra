// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TypedAssignment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TypedAssignment");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.SingleTarget lhs;
  
  public final hydra.ext.python.syntax.Expression type;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.AnnotatedRhs> rhs;
  
  public TypedAssignment (hydra.ext.python.syntax.SingleTarget lhs, hydra.ext.python.syntax.Expression type, hydra.util.Opt<hydra.ext.python.syntax.AnnotatedRhs> rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.type = type;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedAssignment)) {
      return false;
    }
    TypedAssignment o = (TypedAssignment) (other);
    return lhs.equals(o.lhs) && type.equals(o.type) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * type.hashCode() + 5 * rhs.hashCode();
  }
  
  public TypedAssignment withLhs(hydra.ext.python.syntax.SingleTarget lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new TypedAssignment(lhs, type, rhs);
  }
  
  public TypedAssignment withType(hydra.ext.python.syntax.Expression type) {
    java.util.Objects.requireNonNull((type));
    return new TypedAssignment(lhs, type, rhs);
  }
  
  public TypedAssignment withRhs(hydra.util.Opt<hydra.ext.python.syntax.AnnotatedRhs> rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new TypedAssignment(lhs, type, rhs);
  }
}