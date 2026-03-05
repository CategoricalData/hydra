// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class ConstantList_Multiple implements Serializable, Comparable<ConstantList_Multiple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.datalog.syntax.ConstantList_Multiple");
  
  public static final hydra.core.Name CONSTANT = new hydra.core.Name("Constant");
  
  public static final hydra.core.Name CONSTANT_LIST = new hydra.core.Name("ConstantList");
  
  public final hydra.ext.datalog.syntax.Constant Constant;
  
  public final hydra.ext.datalog.syntax.ConstantList ConstantList;
  
  public ConstantList_Multiple (hydra.ext.datalog.syntax.Constant Constant, hydra.ext.datalog.syntax.ConstantList ConstantList) {
    this.Constant = Constant;
    this.ConstantList = ConstantList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantList_Multiple)) {
      return false;
    }
    ConstantList_Multiple o = (ConstantList_Multiple) other;
    return java.util.Objects.equals(
      this.Constant,
      o.Constant) && java.util.Objects.equals(
      this.ConstantList,
      o.ConstantList);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Constant) + 3 * java.util.Objects.hashCode(ConstantList);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstantList_Multiple other) {
    int cmp = 0;
    cmp = ((Comparable) Constant).compareTo(other.Constant);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) ConstantList).compareTo(other.ConstantList);
  }
  
  public ConstantList_Multiple withConstant(hydra.ext.datalog.syntax.Constant Constant) {
    return new ConstantList_Multiple(Constant, ConstantList);
  }
  
  public ConstantList_Multiple withConstantList(hydra.ext.datalog.syntax.ConstantList ConstantList) {
    return new ConstantList_Multiple(Constant, ConstantList);
  }
}
