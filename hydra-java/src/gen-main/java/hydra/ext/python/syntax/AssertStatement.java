// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AssertStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AssertStatement");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION1 = new hydra.core.Name("expression1");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION2 = new hydra.core.Name("expression2");
  
  public final hydra.ext.python.syntax.Expression expression1;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> expression2;
  
  public AssertStatement (hydra.ext.python.syntax.Expression expression1, hydra.util.Opt<hydra.ext.python.syntax.Expression> expression2) {
    java.util.Objects.requireNonNull((expression1));
    java.util.Objects.requireNonNull((expression2));
    this.expression1 = expression1;
    this.expression2 = expression2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AssertStatement)) {
      return false;
    }
    AssertStatement o = (AssertStatement) (other);
    return expression1.equals(o.expression1) && expression2.equals(o.expression2);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression1.hashCode() + 3 * expression2.hashCode();
  }
  
  public AssertStatement withExpression1(hydra.ext.python.syntax.Expression expression1) {
    java.util.Objects.requireNonNull((expression1));
    return new AssertStatement(expression1, expression2);
  }
  
  public AssertStatement withExpression2(hydra.util.Opt<hydra.ext.python.syntax.Expression> expression2) {
    java.util.Objects.requireNonNull((expression2));
    return new AssertStatement(expression1, expression2);
  }
}