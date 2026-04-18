// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class TryStatement_Simple implements Serializable, Comparable<TryStatement_Simple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.TryStatement_Simple");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name CATCHES = new hydra.core.Name("catches");

  public final hydra.java.syntax.Block block;

  public final hydra.java.syntax.Catches catches;

  public TryStatement_Simple (hydra.java.syntax.Block block, hydra.java.syntax.Catches catches) {
    this.block = block;
    this.catches = catches;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryStatement_Simple)) {
      return false;
    }
    TryStatement_Simple o = (TryStatement_Simple) other;
    return java.util.Objects.equals(
      this.block,
      o.block) && java.util.Objects.equals(
      this.catches,
      o.catches);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(block) + 3 * java.util.Objects.hashCode(catches);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TryStatement_Simple other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      block,
      other.block);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      catches,
      other.catches);
  }

  public TryStatement_Simple withBlock(hydra.java.syntax.Block block) {
    return new TryStatement_Simple(block, catches);
  }

  public TryStatement_Simple withCatches(hydra.java.syntax.Catches catches) {
    return new TryStatement_Simple(block, catches);
  }
}
