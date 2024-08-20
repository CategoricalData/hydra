// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class InlineShapeAtom_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.InlineShapeAtom.Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_SHAPE_OR_REF = new hydra.core.Name("inlineShapeOrRef");
  
  public static final hydra.core.Name FIELD_NAME_NODE_CONSTRAINT = new hydra.core.Name("nodeConstraint");
  
  public final hydra.ext.io.shex.syntax.InlineShapeOrRef inlineShapeOrRef;
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.NodeConstraint> nodeConstraint;
  
  public InlineShapeAtom_Sequence2 (hydra.ext.io.shex.syntax.InlineShapeOrRef inlineShapeOrRef, hydra.util.Opt<hydra.ext.io.shex.syntax.NodeConstraint> nodeConstraint) {
    java.util.Objects.requireNonNull((inlineShapeOrRef));
    java.util.Objects.requireNonNull((nodeConstraint));
    this.inlineShapeOrRef = inlineShapeOrRef;
    this.nodeConstraint = nodeConstraint;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineShapeAtom_Sequence2)) {
      return false;
    }
    InlineShapeAtom_Sequence2 o = (InlineShapeAtom_Sequence2) (other);
    return inlineShapeOrRef.equals(o.inlineShapeOrRef) && nodeConstraint.equals(o.nodeConstraint);
  }
  
  @Override
  public int hashCode() {
    return 2 * inlineShapeOrRef.hashCode() + 3 * nodeConstraint.hashCode();
  }
  
  public InlineShapeAtom_Sequence2 withInlineShapeOrRef(hydra.ext.io.shex.syntax.InlineShapeOrRef inlineShapeOrRef) {
    java.util.Objects.requireNonNull((inlineShapeOrRef));
    return new InlineShapeAtom_Sequence2(inlineShapeOrRef, nodeConstraint);
  }
  
  public InlineShapeAtom_Sequence2 withNodeConstraint(hydra.util.Opt<hydra.ext.io.shex.syntax.NodeConstraint> nodeConstraint) {
    java.util.Objects.requireNonNull((nodeConstraint));
    return new InlineShapeAtom_Sequence2(inlineShapeOrRef, nodeConstraint);
  }
}