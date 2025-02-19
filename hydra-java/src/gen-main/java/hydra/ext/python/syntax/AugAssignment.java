// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AugAssignment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AugAssignment");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_AUGASSIGN = new hydra.core.Name("augassign");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.SingleTarget lhs;
  
  public final hydra.ext.python.syntax.AugAssign augassign;
  
  public final hydra.ext.python.syntax.AnnotatedRhs rhs;
  
  public AugAssignment (hydra.ext.python.syntax.SingleTarget lhs, hydra.ext.python.syntax.AugAssign augassign, hydra.ext.python.syntax.AnnotatedRhs rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((augassign));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.augassign = augassign;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AugAssignment)) {
      return false;
    }
    AugAssignment o = (AugAssignment) (other);
    return lhs.equals(o.lhs) && augassign.equals(o.augassign) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * augassign.hashCode() + 5 * rhs.hashCode();
  }
  
  public AugAssignment withLhs(hydra.ext.python.syntax.SingleTarget lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new AugAssignment(lhs, augassign, rhs);
  }
  
  public AugAssignment withAugassign(hydra.ext.python.syntax.AugAssign augassign) {
    java.util.Objects.requireNonNull((augassign));
    return new AugAssignment(lhs, augassign, rhs);
  }
  
  public AugAssignment withRhs(hydra.ext.python.syntax.AnnotatedRhs rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new AugAssignment(lhs, augassign, rhs);
  }
}