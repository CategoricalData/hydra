package hydra.ext.shex.syntax;

public class InlineShapeAtom_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeAtom.Sequence");
  
  public final hydra.ext.shex.syntax.NodeConstraint nodeConstraint;
  
  public final java.util.Optional<hydra.ext.shex.syntax.InlineShapeOrRef> inlineShapeOrRef;
  
  public InlineShapeAtom_Sequence (hydra.ext.shex.syntax.NodeConstraint nodeConstraint, java.util.Optional<hydra.ext.shex.syntax.InlineShapeOrRef> inlineShapeOrRef) {
    this.nodeConstraint = nodeConstraint;
    this.inlineShapeOrRef = inlineShapeOrRef;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAtom_Sequence)) {
      return false;
    }
    InlineShapeAtom_Sequence o = (InlineShapeAtom_Sequence) (other);
    return nodeConstraint.equals(o.nodeConstraint) && inlineShapeOrRef.equals(o.inlineShapeOrRef);
  }
  
  @Override
  public int hashCode() {
    return 2 * nodeConstraint.hashCode() + 3 * inlineShapeOrRef.hashCode();
  }
  
  public InlineShapeAtom_Sequence withNodeConstraint(hydra.ext.shex.syntax.NodeConstraint nodeConstraint) {
    return new InlineShapeAtom_Sequence(nodeConstraint, inlineShapeOrRef);
  }
  
  public InlineShapeAtom_Sequence withInlineShapeOrRef(java.util.Optional<hydra.ext.shex.syntax.InlineShapeOrRef> inlineShapeOrRef) {
    return new InlineShapeAtom_Sequence(nodeConstraint, inlineShapeOrRef);
  }
}