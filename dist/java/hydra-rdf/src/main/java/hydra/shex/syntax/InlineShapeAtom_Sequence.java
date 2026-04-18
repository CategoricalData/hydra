// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class InlineShapeAtom_Sequence implements Serializable, Comparable<InlineShapeAtom_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.InlineShapeAtom_Sequence");

  public static final hydra.core.Name NODE_CONSTRAINT = new hydra.core.Name("NodeConstraint");

  public static final hydra.core.Name INLINE_SHAPE_OR_REF = new hydra.core.Name("InlineShapeOrRef");

  public final hydra.shex.syntax.NodeConstraint NodeConstraint;

  public final hydra.util.Maybe<hydra.shex.syntax.InlineShapeOrRef> InlineShapeOrRef;

  public InlineShapeAtom_Sequence (hydra.shex.syntax.NodeConstraint NodeConstraint, hydra.util.Maybe<hydra.shex.syntax.InlineShapeOrRef> InlineShapeOrRef) {
    this.NodeConstraint = NodeConstraint;
    this.InlineShapeOrRef = InlineShapeOrRef;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAtom_Sequence)) {
      return false;
    }
    InlineShapeAtom_Sequence o = (InlineShapeAtom_Sequence) other;
    return java.util.Objects.equals(
      this.NodeConstraint,
      o.NodeConstraint) && java.util.Objects.equals(
      this.InlineShapeOrRef,
      o.InlineShapeOrRef);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(NodeConstraint) + 3 * java.util.Objects.hashCode(InlineShapeOrRef);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InlineShapeAtom_Sequence other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      NodeConstraint,
      other.NodeConstraint);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      InlineShapeOrRef,
      other.InlineShapeOrRef);
  }

  public InlineShapeAtom_Sequence withNodeConstraint(hydra.shex.syntax.NodeConstraint NodeConstraint) {
    return new InlineShapeAtom_Sequence(NodeConstraint, InlineShapeOrRef);
  }

  public InlineShapeAtom_Sequence withInlineShapeOrRef(hydra.util.Maybe<hydra.shex.syntax.InlineShapeOrRef> InlineShapeOrRef) {
    return new InlineShapeAtom_Sequence(NodeConstraint, InlineShapeOrRef);
  }
}
