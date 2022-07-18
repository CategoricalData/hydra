package hydra.ext.datalog.syntax;

public class ConstantList_Multiple {
  public final Constant constant;
  
  public final ConstantList constantList;
  
  public ConstantList_Multiple (Constant constant, ConstantList constantList) {
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
  
  public ConstantList_Multiple withConstant(Constant constant) {
    return new ConstantList_Multiple(constant, constantList);
  }
  
  public ConstantList_Multiple withConstantList(ConstantList constantList) {
    return new ConstantList_Multiple(constant, constantList);
  }
}