// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class SwitchStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SwitchStatement");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public final hydra.ext.java.syntax.Expression cond;
  
  public final hydra.ext.java.syntax.SwitchBlock block;
  
  public SwitchStatement (hydra.ext.java.syntax.Expression cond, hydra.ext.java.syntax.SwitchBlock block) {
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((block));
    this.cond = cond;
    this.block = block;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SwitchStatement)) {
      return false;
    }
    SwitchStatement o = (SwitchStatement) (other);
    return cond.equals(o.cond) && block.equals(o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode() + 3 * block.hashCode();
  }
  
  public SwitchStatement withCond(hydra.ext.java.syntax.Expression cond) {
    java.util.Objects.requireNonNull((cond));
    return new SwitchStatement(cond, block);
  }
  
  public SwitchStatement withBlock(hydra.ext.java.syntax.SwitchBlock block) {
    java.util.Objects.requireNonNull((block));
    return new SwitchStatement(cond, block);
  }
}