package hydra.ext.haskell.ast;

public class Type_Infix {
  public final hydra.ext.haskell.ast.Type lhs;
  
  public final hydra.ext.haskell.ast.Operator operator;
  
  public final hydra.ext.haskell.ast.Operator rhs;
  
  public Type_Infix (hydra.ext.haskell.ast.Type lhs, hydra.ext.haskell.ast.Operator operator, hydra.ext.haskell.ast.Operator rhs) {
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
  
  public Type_Infix withLhs(hydra.ext.haskell.ast.Type lhs) {
    return new Type_Infix(lhs, operator, rhs);
  }
  
  public Type_Infix withOperator(hydra.ext.haskell.ast.Operator operator) {
    return new Type_Infix(lhs, operator, rhs);
  }
  
  public Type_Infix withRhs(hydra.ext.haskell.ast.Operator rhs) {
    return new Type_Infix(lhs, operator, rhs);
  }
}