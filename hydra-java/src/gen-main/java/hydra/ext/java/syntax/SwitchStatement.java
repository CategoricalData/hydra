// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SwitchStatement implements Serializable, Comparable<SwitchStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SwitchStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public final hydra.ext.java.syntax.Expression cond;
  
  public final hydra.ext.java.syntax.SwitchBlock block;
  
  public SwitchStatement (hydra.ext.java.syntax.Expression cond, hydra.ext.java.syntax.SwitchBlock block) {
    this.cond = cond;
    this.block = block;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchStatement)) {
      return false;
    }
    SwitchStatement o = (SwitchStatement) other;
    return java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.block,
      o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cond) + 3 * java.util.Objects.hashCode(block);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SwitchStatement other) {
    int cmp = 0;
    cmp = ((Comparable) cond).compareTo(other.cond);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) block).compareTo(other.block);
  }
  
  public SwitchStatement withCond(hydra.ext.java.syntax.Expression cond) {
    return new SwitchStatement(cond, block);
  }
  
  public SwitchStatement withBlock(hydra.ext.java.syntax.SwitchBlock block) {
    return new SwitchStatement(cond, block);
  }
}
