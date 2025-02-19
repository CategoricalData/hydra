// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class UntypedAssignment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.UntypedAssignment");
  
  public static final hydra.core.Name FIELD_NAME_TARGETS = new hydra.core.Name("targets");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final java.util.List<hydra.ext.python.syntax.StarTarget> targets;
  
  public final hydra.ext.python.syntax.AnnotatedRhs rhs;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public UntypedAssignment (java.util.List<hydra.ext.python.syntax.StarTarget> targets, hydra.ext.python.syntax.AnnotatedRhs rhs, hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((targets));
    java.util.Objects.requireNonNull((rhs));
    java.util.Objects.requireNonNull((typeComment));
    this.targets = targets;
    this.rhs = rhs;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UntypedAssignment)) {
      return false;
    }
    UntypedAssignment o = (UntypedAssignment) (other);
    return targets.equals(o.targets) && rhs.equals(o.rhs) && typeComment.equals(o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * targets.hashCode() + 3 * rhs.hashCode() + 5 * typeComment.hashCode();
  }
  
  public UntypedAssignment withTargets(java.util.List<hydra.ext.python.syntax.StarTarget> targets) {
    java.util.Objects.requireNonNull((targets));
    return new UntypedAssignment(targets, rhs, typeComment);
  }
  
  public UntypedAssignment withRhs(hydra.ext.python.syntax.AnnotatedRhs rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new UntypedAssignment(targets, rhs, typeComment);
  }
  
  public UntypedAssignment withTypeComment(hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((typeComment));
    return new UntypedAssignment(targets, rhs, typeComment);
  }
}