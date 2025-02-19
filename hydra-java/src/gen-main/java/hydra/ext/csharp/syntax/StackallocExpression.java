// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class StackallocExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.StackallocExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.UnmanagedType> type;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> expression;
  
  public final java.util.List<hydra.ext.csharp.syntax.Expression> initializer;
  
  public StackallocExpression (hydra.util.Opt<hydra.ext.csharp.syntax.UnmanagedType> type, hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> expression, java.util.List<hydra.ext.csharp.syntax.Expression> initializer) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((initializer));
    this.type = type;
    this.expression = expression;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StackallocExpression)) {
      return false;
    }
    StackallocExpression o = (StackallocExpression) (other);
    return type.equals(o.type) && expression.equals(o.expression) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * expression.hashCode() + 5 * initializer.hashCode();
  }
  
  public StackallocExpression withType(hydra.util.Opt<hydra.ext.csharp.syntax.UnmanagedType> type) {
    java.util.Objects.requireNonNull((type));
    return new StackallocExpression(type, expression, initializer);
  }
  
  public StackallocExpression withExpression(hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> expression) {
    java.util.Objects.requireNonNull((expression));
    return new StackallocExpression(type, expression, initializer);
  }
  
  public StackallocExpression withInitializer(java.util.List<hydra.ext.csharp.syntax.Expression> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new StackallocExpression(type, expression, initializer);
  }
}