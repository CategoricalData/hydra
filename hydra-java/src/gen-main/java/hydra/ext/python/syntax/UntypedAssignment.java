// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class UntypedAssignment implements Serializable, Comparable<UntypedAssignment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.UntypedAssignment");
  
  public static final hydra.core.Name FIELD_NAME_TARGETS = new hydra.core.Name("targets");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final java.util.List<hydra.ext.python.syntax.StarTarget> targets;
  
  public final hydra.ext.python.syntax.AnnotatedRhs rhs;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public UntypedAssignment (java.util.List<hydra.ext.python.syntax.StarTarget> targets, hydra.ext.python.syntax.AnnotatedRhs rhs, hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    this.targets = targets;
    this.rhs = rhs;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UntypedAssignment)) {
      return false;
    }
    UntypedAssignment o = (UntypedAssignment) other;
    return java.util.Objects.equals(
      this.targets,
      o.targets) && java.util.Objects.equals(
      this.rhs,
      o.rhs) && java.util.Objects.equals(
      this.typeComment,
      o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(targets) + 3 * java.util.Objects.hashCode(rhs) + 5 * java.util.Objects.hashCode(typeComment);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UntypedAssignment other) {
    int cmp = 0;
    cmp = Integer.compare(
      targets.hashCode(),
      other.targets.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) rhs).compareTo(other.rhs);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeComment.hashCode(),
      other.typeComment.hashCode());
  }
  
  public UntypedAssignment withTargets(java.util.List<hydra.ext.python.syntax.StarTarget> targets) {
    return new UntypedAssignment(targets, rhs, typeComment);
  }
  
  public UntypedAssignment withRhs(hydra.ext.python.syntax.AnnotatedRhs rhs) {
    return new UntypedAssignment(targets, rhs, typeComment);
  }
  
  public UntypedAssignment withTypeComment(hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    return new UntypedAssignment(targets, rhs, typeComment);
  }
}
