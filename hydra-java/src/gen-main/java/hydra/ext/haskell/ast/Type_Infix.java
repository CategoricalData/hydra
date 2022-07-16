package hydra.ext.haskell.ast;

public class Type_Infix {
  public final Type lhs;
  
  public final Operator operator;
  
  public final Operator rhs;
  
  public Type_Infix (Type lhs, Operator operator, Operator rhs) {
    this.lhs = lhs;
    this.operator = operator;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Infix)) {
      return false;
    }
    Type_Infix o = (Type_Infix) (other);
    return lhs.equals(o.lhs) && operator.equals(o.operator) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * operator.hashCode() + 5 * rhs.hashCode();
  }
  
  public Type_Infix withLhs(Type lhs) {
    return new Type_Infix(lhs, operator, rhs);
  }
  
  public Type_Infix withOperator(Operator operator) {
    return new Type_Infix(lhs, operator, rhs);
  }
  
  public Type_Infix withRhs(Operator rhs) {
    return new Type_Infix(lhs, operator, rhs);
  }
}