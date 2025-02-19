// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ForStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ForStatement");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public static final hydra.core.Name FIELD_NAME_CONDITION = new hydra.core.Name("condition");
  
  public static final hydra.core.Name FIELD_NAME_ITERATOR = new hydra.core.Name("iterator");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ForInitializer> initializer;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.BooleanExpression> condition;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.StatementExpressionList> iterator;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement body;
  
  public ForStatement (hydra.util.Opt<hydra.ext.csharp.syntax.ForInitializer> initializer, hydra.util.Opt<hydra.ext.csharp.syntax.BooleanExpression> condition, hydra.util.Opt<hydra.ext.csharp.syntax.StatementExpressionList> iterator, hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((initializer));
    java.util.Objects.requireNonNull((condition));
    java.util.Objects.requireNonNull((iterator));
    java.util.Objects.requireNonNull((body));
    this.initializer = initializer;
    this.condition = condition;
    this.iterator = iterator;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForStatement)) {
      return false;
    }
    ForStatement o = (ForStatement) (other);
    return initializer.equals(o.initializer) && condition.equals(o.condition) && iterator.equals(o.iterator) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * initializer.hashCode() + 3 * condition.hashCode() + 5 * iterator.hashCode() + 7 * body.hashCode();
  }
  
  public ForStatement withInitializer(hydra.util.Opt<hydra.ext.csharp.syntax.ForInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new ForStatement(initializer, condition, iterator, body);
  }
  
  public ForStatement withCondition(hydra.util.Opt<hydra.ext.csharp.syntax.BooleanExpression> condition) {
    java.util.Objects.requireNonNull((condition));
    return new ForStatement(initializer, condition, iterator, body);
  }
  
  public ForStatement withIterator(hydra.util.Opt<hydra.ext.csharp.syntax.StatementExpressionList> iterator) {
    java.util.Objects.requireNonNull((iterator));
    return new ForStatement(initializer, condition, iterator, body);
  }
  
  public ForStatement withBody(hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((body));
    return new ForStatement(initializer, condition, iterator, body);
  }
}