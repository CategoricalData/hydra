package hydra.ext.shex.syntax;

public class ShapeAtom_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeAtom.Sequence");
  
  public final hydra.ext.shex.syntax.NodeConstraint nodeConstraint;
  
  public final java.util.Optional<hydra.ext.shex.syntax.ShapeOrRef> shapeOrRef;
  
  public ShapeAtom_Sequence (hydra.ext.shex.syntax.NodeConstraint nodeConstraint, java.util.Optional<hydra.ext.shex.syntax.ShapeOrRef> shapeOrRef) {
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
  
  public ShapeAtom_Sequence withNodeConstraint(hydra.ext.shex.syntax.NodeConstraint nodeConstraint) {
    return new ShapeAtom_Sequence(nodeConstraint, shapeOrRef);
  }
  
  public ShapeAtom_Sequence withShapeOrRef(java.util.Optional<hydra.ext.shex.syntax.ShapeOrRef> shapeOrRef) {
    return new ShapeAtom_Sequence(nodeConstraint, shapeOrRef);
  }
}