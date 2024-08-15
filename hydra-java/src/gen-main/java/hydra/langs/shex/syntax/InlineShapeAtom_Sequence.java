// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class InlineShapeAtom_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.InlineShapeAtom.Sequence");
  
  public static final hydra.core.Name FIELD_NAME_NODE_CONSTRAINT = new hydra.core.Name("nodeConstraint");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_SHAPE_OR_REF = new hydra.core.Name("inlineShapeOrRef");
  
  public final hydra.langs.shex.syntax.NodeConstraint nodeConstraint;
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.InlineShapeOrRef> inlineShapeOrRef;
  
  public InlineShapeAtom_Sequence (hydra.langs.shex.syntax.NodeConstraint nodeConstraint, hydra.util.Opt<hydra.langs.shex.syntax.InlineShapeOrRef> inlineShapeOrRef) {
    java.util.Objects.requireNonNull((nodeConstraint));
    java.util.Objects.requireNonNull((inlineShapeOrRef));
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
  
  public InlineShapeAtom_Sequence withNodeConstraint(hydra.langs.shex.syntax.NodeConstraint nodeConstraint) {
    java.util.Objects.requireNonNull((nodeConstraint));
    return new InlineShapeAtom_Sequence(nodeConstraint, inlineShapeOrRef);
  }
  
  public InlineShapeAtom_Sequence withInlineShapeOrRef(hydra.util.Opt<hydra.langs.shex.syntax.InlineShapeOrRef> inlineShapeOrRef) {
    java.util.Objects.requireNonNull((inlineShapeOrRef));
    return new InlineShapeAtom_Sequence(nodeConstraint, inlineShapeOrRef);
  }
}