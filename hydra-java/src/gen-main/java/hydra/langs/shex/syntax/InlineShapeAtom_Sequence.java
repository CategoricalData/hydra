// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class InlineShapeAtom_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.InlineShapeAtom.Sequence");
  
  public final hydra.langs.shex.syntax.NodeConstraint nodeConstraint;
  
  public final hydra.util.Opt<hydra.langs.shex.syntax.InlineShapeOrRef> inlineShapeOrRef;
  
  public InlineShapeAtom_Sequence (hydra.langs.shex.syntax.NodeConstraint nodeConstraint, hydra.util.Opt<hydra.langs.shex.syntax.InlineShapeOrRef> inlineShapeOrRef) {
    if (nodeConstraint == null) {
      throw new IllegalArgumentException("null value for 'nodeConstraint' argument");
    }
    if (inlineShapeOrRef == null) {
      throw new IllegalArgumentException("null value for 'inlineShapeOrRef' argument");
    }
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
    if (nodeConstraint == null) {
      throw new IllegalArgumentException("null value for 'nodeConstraint' argument");
    }
    return new InlineShapeAtom_Sequence(nodeConstraint, inlineShapeOrRef);
  }
  
  public InlineShapeAtom_Sequence withInlineShapeOrRef(hydra.util.Opt<hydra.langs.shex.syntax.InlineShapeOrRef> inlineShapeOrRef) {
    if (inlineShapeOrRef == null) {
      throw new IllegalArgumentException("null value for 'inlineShapeOrRef' argument");
    }
    return new InlineShapeAtom_Sequence(nodeConstraint, inlineShapeOrRef);
  }
}