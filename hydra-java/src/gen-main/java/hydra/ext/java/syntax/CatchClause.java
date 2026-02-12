// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CatchClause implements Serializable, Comparable<CatchClause> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CatchClause");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.CatchFormalParameter> parameter;
  
  public final hydra.ext.java.syntax.Block block;
  
  public CatchClause (hydra.util.Maybe<hydra.ext.java.syntax.CatchFormalParameter> parameter, hydra.ext.java.syntax.Block block) {
    this.parameter = parameter;
    this.block = block;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchClause)) {
      return false;
    }
    CatchClause o = (CatchClause) other;
    return java.util.Objects.equals(
      this.parameter,
      o.parameter) && java.util.Objects.equals(
      this.block,
      o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameter) + 3 * java.util.Objects.hashCode(block);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CatchClause other) {
    int cmp = 0;
    cmp = Integer.compare(
      parameter.hashCode(),
      other.parameter.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) block).compareTo(other.block);
  }
  
  public CatchClause withParameter(hydra.util.Maybe<hydra.ext.java.syntax.CatchFormalParameter> parameter) {
    return new CatchClause(parameter, block);
  }
  
  public CatchClause withBlock(hydra.ext.java.syntax.Block block) {
    return new CatchClause(parameter, block);
  }
}
