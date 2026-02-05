// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An application-style declaration head
 */
public class ApplicationDeclarationHead implements Serializable, Comparable<ApplicationDeclarationHead> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ApplicationDeclarationHead");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_OPERAND = new hydra.core.Name("operand");
  
  /**
   * The function being applied
   */
  public final hydra.ext.haskell.ast.DeclarationHead function;
  
  /**
   * The type variable operand
   */
  public final hydra.ext.haskell.ast.Variable operand;
  
  public ApplicationDeclarationHead (hydra.ext.haskell.ast.DeclarationHead function, hydra.ext.haskell.ast.Variable operand) {
    this.function = function;
    this.operand = operand;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationDeclarationHead)) {
      return false;
    }
    ApplicationDeclarationHead o = (ApplicationDeclarationHead) (other);
    return java.util.Objects.equals(
      this.function,
      o.function) && java.util.Objects.equals(
      this.operand,
      o.operand);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(function) + 3 * java.util.Objects.hashCode(operand);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ApplicationDeclarationHead other) {
    int cmp = 0;
    cmp = ((Comparable) (function)).compareTo(other.function);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (operand)).compareTo(other.operand);
  }
  
  public ApplicationDeclarationHead withFunction(hydra.ext.haskell.ast.DeclarationHead function) {
    return new ApplicationDeclarationHead(function, operand);
  }
  
  public ApplicationDeclarationHead withOperand(hydra.ext.haskell.ast.Variable operand) {
    return new ApplicationDeclarationHead(function, operand);
  }
}
