// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ClassInstanceCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ClassInstanceCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.util.Opt<hydra.langs.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier;
  
  public final hydra.langs.java.syntax.UnqualifiedClassInstanceCreationExpression expression;
  
  public ClassInstanceCreationExpression (hydra.util.Opt<hydra.langs.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier, hydra.langs.java.syntax.UnqualifiedClassInstanceCreationExpression expression) {
    java.util.Objects.requireNonNull((qualifier));
    java.util.Objects.requireNonNull((expression));
    this.qualifier = qualifier;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassInstanceCreationExpression)) {
      return false;
    }
    ClassInstanceCreationExpression o = (ClassInstanceCreationExpression) (other);
    return qualifier.equals(o.qualifier) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifier.hashCode() + 3 * expression.hashCode();
  }
  
  public ClassInstanceCreationExpression withQualifier(hydra.util.Opt<hydra.langs.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier) {
    java.util.Objects.requireNonNull((qualifier));
    return new ClassInstanceCreationExpression(qualifier, expression);
  }
  
  public ClassInstanceCreationExpression withExpression(hydra.langs.java.syntax.UnqualifiedClassInstanceCreationExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ClassInstanceCreationExpression(qualifier, expression);
  }
}