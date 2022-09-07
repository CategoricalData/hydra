package hydra.ext.datalog.syntax;

public class ConstantList_Multiple {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.ConstantList.Multiple");
  
  public final hydra.ext.datalog.syntax.Constant constant;
  
  public final hydra.ext.datalog.syntax.ConstantList constantList;
  
  public ConstantList_Multiple (hydra.ext.datalog.syntax.Constant constant, hydra.ext.datalog.syntax.ConstantList constantList) {
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
    return new ConstantList_Multiple(constant, constantList);
  }
  
  public ConstantList_Multiple withConstantList(hydra.ext.datalog.syntax.ConstantList constantList) {
    return new ConstantList_Multiple(constant, constantList);
  }
}