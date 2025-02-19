// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShapeAtom_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAtom_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_NODE_CONSTRAINT = new hydra.core.Name("nodeConstraint");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_OR_REF = new hydra.core.Name("shapeOrRef");
  
  public final hydra.ext.io.shex.syntax.NodeConstraint nodeConstraint;
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.ShapeOrRef> shapeOrRef;
  
  public ShapeAtom_Sequence (hydra.ext.io.shex.syntax.NodeConstraint nodeConstraint, hydra.util.Opt<hydra.ext.io.shex.syntax.ShapeOrRef> shapeOrRef) {
    java.util.Objects.requireNonNull((nodeConstraint));
    java.util.Objects.requireNonNull((shapeOrRef));
    this.nodeConstraint = nodeConstraint;
    this.shapeOrRef = shapeOrRef;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapeAtom_Sequence)) {
      return false;
    }
    ShapeAtom_Sequence o = (ShapeAtom_Sequence) (other);
    return nodeConstraint.equals(o.nodeConstraint) && shapeOrRef.equals(o.shapeOrRef);
  }
  
  @Override
  public int hashCode() {
    return 2 * nodeConstraint.hashCode() + 3 * shapeOrRef.hashCode();
  }
  
  public ShapeAtom_Sequence withNodeConstraint(hydra.ext.io.shex.syntax.NodeConstraint nodeConstraint) {
    java.util.Objects.requireNonNull((nodeConstraint));
    return new ShapeAtom_Sequence(nodeConstraint, shapeOrRef);
  }
  
  public ShapeAtom_Sequence withShapeOrRef(hydra.util.Opt<hydra.ext.io.shex.syntax.ShapeOrRef> shapeOrRef) {
    java.util.Objects.requireNonNull((shapeOrRef));
    return new ShapeAtom_Sequence(nodeConstraint, shapeOrRef);
  }
}