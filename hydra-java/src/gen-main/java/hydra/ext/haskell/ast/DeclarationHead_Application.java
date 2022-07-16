package hydra.ext.haskell.ast;

/**
 * An application-style declaration head
 */
public class DeclarationHead_Application {
  public final DeclarationHead function;
  
  public final Variable operand;
  
  public DeclarationHead_Application (DeclarationHead function, Variable operand) {
    this.function = function;
    this.operand = operand;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeclarationHead_Application)) {
      return false;
    }
    DeclarationHead_Application o = (DeclarationHead_Application) (other);
    return function.equals(o.function) && operand.equals(o.operand);
  }
  
  @Override
  public int hashCode() {
    return 2 * function.hashCode() + 3 * operand.hashCode();
  }
  
  public DeclarationHead_Application withFunction(DeclarationHead function) {
    return new DeclarationHead_Application(function, operand);
  }
  
  public DeclarationHead_Application withOperand(Variable operand) {
    return new DeclarationHead_Application(function, operand);
  }
}