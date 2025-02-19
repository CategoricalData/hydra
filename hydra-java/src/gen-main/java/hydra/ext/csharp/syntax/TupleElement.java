// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class TupleElement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TupleElement");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name;
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public TupleElement (hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name, hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((expression));
    this.name = name;
    this.expression = expression;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TupleElement)) {
      return false;
    }
    TupleElement o = (TupleElement) (other);
    return name.equals(o.name) && expression.equals(o.expression);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * expression.hashCode();
  }
  
  public TupleElement withName(hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name) {
    java.util.Objects.requireNonNull((name));
    return new TupleElement(name, expression);
  }
  
  public TupleElement withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new TupleElement(name, expression);
  }
}