// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AugAssignment implements Serializable, Comparable<AugAssignment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AugAssignment");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_AUGASSIGN = new hydra.core.Name("augassign");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.python.syntax.SingleTarget lhs;
  
  public final hydra.ext.python.syntax.AugAssign augassign;
  
  public final hydra.ext.python.syntax.AnnotatedRhs rhs;
  
  public AugAssignment (hydra.ext.python.syntax.SingleTarget lhs, hydra.ext.python.syntax.AugAssign augassign, hydra.ext.python.syntax.AnnotatedRhs rhs) {
    this.lhs = lhs;
    this.augassign = augassign;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AugAssignment)) {
      return false;
    }
    AugAssignment o = (AugAssignment) other;
    return java.util.Objects.equals(
      this.lhs,
      o.lhs) && java.util.Objects.equals(
      this.augassign,
      o.augassign) && java.util.Objects.equals(
      this.rhs,
      o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lhs) + 3 * java.util.Objects.hashCode(augassign) + 5 * java.util.Objects.hashCode(rhs);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AugAssignment other) {
    int cmp = 0;
    cmp = ((Comparable) lhs).compareTo(other.lhs);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) augassign).compareTo(other.augassign);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) rhs).compareTo(other.rhs);
  }
  
  public AugAssignment withLhs(hydra.ext.python.syntax.SingleTarget lhs) {
    return new AugAssignment(lhs, augassign, rhs);
  }
  
  public AugAssignment withAugassign(hydra.ext.python.syntax.AugAssign augassign) {
    return new AugAssignment(lhs, augassign, rhs);
  }
  
  public AugAssignment withRhs(hydra.ext.python.syntax.AnnotatedRhs rhs) {
    return new AugAssignment(lhs, augassign, rhs);
  }
}
