// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class InlineShapeAtom_Sequence2 implements Serializable, Comparable<InlineShapeAtom_Sequence2> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence2");

  public static final hydra.core.Name INLINE_SHAPE_OR_REF = new hydra.core.Name("InlineShapeOrRef");

  public static final hydra.core.Name NODE_CONSTRAINT = new hydra.core.Name("NodeConstraint");

  public final hydra.ext.io.shex.syntax.InlineShapeOrRef InlineShapeOrRef;

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.NodeConstraint> NodeConstraint;

  public InlineShapeAtom_Sequence2 (hydra.ext.io.shex.syntax.InlineShapeOrRef InlineShapeOrRef, hydra.util.Maybe<hydra.ext.io.shex.syntax.NodeConstraint> NodeConstraint) {
    this.InlineShapeOrRef = InlineShapeOrRef;
    this.NodeConstraint = NodeConstraint;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAtom_Sequence2)) {
      return false;
    }
    InlineShapeAtom_Sequence2 o = (InlineShapeAtom_Sequence2) other;
    return java.util.Objects.equals(
      this.InlineShapeOrRef,
      o.InlineShapeOrRef) && java.util.Objects.equals(
      this.NodeConstraint,
      o.NodeConstraint);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(InlineShapeOrRef) + 3 * java.util.Objects.hashCode(NodeConstraint);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InlineShapeAtom_Sequence2 other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      InlineShapeOrRef,
      other.InlineShapeOrRef);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      NodeConstraint,
      other.NodeConstraint);
  }

  public InlineShapeAtom_Sequence2 withInlineShapeOrRef(hydra.ext.io.shex.syntax.InlineShapeOrRef InlineShapeOrRef) {
    return new InlineShapeAtom_Sequence2(InlineShapeOrRef, NodeConstraint);
  }

  public InlineShapeAtom_Sequence2 withNodeConstraint(hydra.util.Maybe<hydra.ext.io.shex.syntax.NodeConstraint> NodeConstraint) {
    return new InlineShapeAtom_Sequence2(InlineShapeOrRef, NodeConstraint);
  }
}
