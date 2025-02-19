// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class CatchClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.CatchClause");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public final hydra.util.Opt<hydra.ext.java.syntax.CatchFormalParameter> parameter;
  
  public final hydra.ext.java.syntax.Block block;
  
  public CatchClause (hydra.util.Opt<hydra.ext.java.syntax.CatchFormalParameter> parameter, hydra.ext.java.syntax.Block block) {
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((block));
    this.parameter = parameter;
    this.block = block;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchClause)) {
      return false;
    }
    CatchClause o = (CatchClause) (other);
    return parameter.equals(o.parameter) && block.equals(o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * block.hashCode();
  }
  
  public CatchClause withParameter(hydra.util.Opt<hydra.ext.java.syntax.CatchFormalParameter> parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new CatchClause(parameter, block);
  }
  
  public CatchClause withBlock(hydra.ext.java.syntax.Block block) {
    java.util.Objects.requireNonNull((block));
    return new CatchClause(parameter, block);
  }
}