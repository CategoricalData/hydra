// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An application-style declaration head
 */
public class DeclarationHead_Application implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.DeclarationHead.Application");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  public final hydra.ext.haskell.ast.DeclarationHead function;
  
  public final hydra.ext.haskell.ast.Variable operand;
  
  public DeclarationHead_Application (hydra.ext.haskell.ast.DeclarationHead function, hydra.ext.haskell.ast.Variable operand) {
    java.util.Objects.requireNonNull((function));
    java.util.Objects.requireNonNull((operand));
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
  
  public DeclarationHead_Application withFunction(hydra.ext.haskell.ast.DeclarationHead function) {
    java.util.Objects.requireNonNull((function));
    return new DeclarationHead_Application(function, operand);
  }
  
  public DeclarationHead_Application withOperand(hydra.ext.haskell.ast.Variable operand) {
    java.util.Objects.requireNonNull((operand));
    return new DeclarationHead_Application(function, operand);
  }
}
