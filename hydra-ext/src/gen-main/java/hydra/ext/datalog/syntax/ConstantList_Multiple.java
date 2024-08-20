// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class ConstantList_Multiple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/datalog/syntax.ConstantList.Multiple");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT_LIST = new hydra.core.Name("constantList");
  
  public final hydra.ext.datalog.syntax.Constant constant;
  
  public final hydra.ext.datalog.syntax.ConstantList constantList;
  
  public ConstantList_Multiple (hydra.ext.datalog.syntax.Constant constant, hydra.ext.datalog.syntax.ConstantList constantList) {
    java.util.Objects.requireNonNull((constant));
    java.util.Objects.requireNonNull((constantList));
    this.constant = constant;
    this.constantList = constantList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantList_Multiple)) {
      return false;
    }
    ConstantList_Multiple o = (ConstantList_Multiple) (other);
    return constant.equals(o.constant) && constantList.equals(o.constantList);
  }
  
  @Override
  public int hashCode() {
    return 2 * constant.hashCode() + 3 * constantList.hashCode();
  }
  
  public ConstantList_Multiple withConstant(hydra.ext.datalog.syntax.Constant constant) {
    java.util.Objects.requireNonNull((constant));
    return new ConstantList_Multiple(constant, constantList);
  }
  
  public ConstantList_Multiple withConstantList(hydra.ext.datalog.syntax.ConstantList constantList) {
    java.util.Objects.requireNonNull((constantList));
    return new ConstantList_Multiple(constant, constantList);
  }
}