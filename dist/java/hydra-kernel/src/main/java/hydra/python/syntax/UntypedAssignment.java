// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class UntypedAssignment implements Serializable, Comparable<UntypedAssignment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.UntypedAssignment");

  public static final hydra.core.Name TARGETS = new hydra.core.Name("targets");

  public static final hydra.core.Name RHS = new hydra.core.Name("rhs");

  public static final hydra.core.Name TYPE_COMMENT = new hydra.core.Name("typeComment");

  public final java.util.List<hydra.python.syntax.StarTarget> targets;

  public final hydra.python.syntax.AnnotatedRhs rhs;

  public final hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment;

  public UntypedAssignment (java.util.List<hydra.python.syntax.StarTarget> targets, hydra.python.syntax.AnnotatedRhs rhs, hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment) {
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
    cmp = hydra.util.Comparing.compare(
      targets,
      other.targets);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      rhs,
      other.rhs);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      typeComment,
      other.typeComment);
  }

  public UntypedAssignment withTargets(java.util.List<hydra.python.syntax.StarTarget> targets) {
    return new UntypedAssignment(targets, rhs, typeComment);
  }

  public UntypedAssignment withRhs(hydra.python.syntax.AnnotatedRhs rhs) {
    return new UntypedAssignment(targets, rhs, typeComment);
  }

  public UntypedAssignment withTypeComment(hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment) {
    return new UntypedAssignment(targets, rhs, typeComment);
  }
}
