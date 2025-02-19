// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class PositionalArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PositionalArgument");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name;
  
  public final hydra.ext.csharp.syntax.AttributeArgumentExpression value;
  
  public PositionalArgument (hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name, hydra.ext.csharp.syntax.AttributeArgumentExpression value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PositionalArgument)) {
      return false;
    }
    PositionalArgument o = (PositionalArgument) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public PositionalArgument withName(hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name) {
    java.util.Objects.requireNonNull((name));
    return new PositionalArgument(name, value);
  }
  
  public PositionalArgument withValue(hydra.ext.csharp.syntax.AttributeArgumentExpression value) {
    java.util.Objects.requireNonNull((value));
    return new PositionalArgument(name, value);
  }
}