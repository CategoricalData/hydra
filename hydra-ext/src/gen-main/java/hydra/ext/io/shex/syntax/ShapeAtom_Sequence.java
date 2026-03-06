// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShapeAtom_Sequence implements Serializable, Comparable<ShapeAtom_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAtom_Sequence");
  
  public static final hydra.core.Name NODE_CONSTRAINT = new hydra.core.Name("NodeConstraint");
  
  public static final hydra.core.Name SHAPE_OR_REF = new hydra.core.Name("ShapeOrRef");
  
  public final hydra.ext.io.shex.syntax.NodeConstraint NodeConstraint;
  
  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.ShapeOrRef> ShapeOrRef;
  
  public ShapeAtom_Sequence (hydra.ext.io.shex.syntax.NodeConstraint NodeConstraint, hydra.util.Maybe<hydra.ext.io.shex.syntax.ShapeOrRef> ShapeOrRef) {
    this.NodeConstraint = NodeConstraint;
    this.ShapeOrRef = ShapeOrRef;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeAtom_Sequence)) {
      return false;
    }
    ShapeAtom_Sequence o = (ShapeAtom_Sequence) other;
    return java.util.Objects.equals(
      this.NodeConstraint,
      o.NodeConstraint) && java.util.Objects.equals(
      this.ShapeOrRef,
      o.ShapeOrRef);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(NodeConstraint) + 3 * java.util.Objects.hashCode(ShapeOrRef);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShapeAtom_Sequence other) {
    int cmp = 0;
    cmp = ((Comparable) NodeConstraint).compareTo(other.NodeConstraint);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      ShapeOrRef.hashCode(),
      other.ShapeOrRef.hashCode());
  }
  
  public ShapeAtom_Sequence withNodeConstraint(hydra.ext.io.shex.syntax.NodeConstraint NodeConstraint) {
    return new ShapeAtom_Sequence(NodeConstraint, ShapeOrRef);
  }
  
  public ShapeAtom_Sequence withShapeOrRef(hydra.util.Maybe<hydra.ext.io.shex.syntax.ShapeOrRef> ShapeOrRef) {
    return new ShapeAtom_Sequence(NodeConstraint, ShapeOrRef);
  }
}
