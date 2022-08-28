package hydra.ext.java.syntax;

public class ClassInstanceCreationExpression {
  public final java.util.Optional<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier;
  
  public final hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression expression;
  
  public ClassInstanceCreationExpression (java.util.Optional<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier, hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression expression) {
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
  
  public ClassInstanceCreationExpression withQualifier(java.util.Optional<hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier> qualifier) {
    return new ClassInstanceCreationExpression(qualifier, expression);
  }
  
  public ClassInstanceCreationExpression withExpression(hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression expression) {
    return new ClassInstanceCreationExpression(qualifier, expression);
  }
}