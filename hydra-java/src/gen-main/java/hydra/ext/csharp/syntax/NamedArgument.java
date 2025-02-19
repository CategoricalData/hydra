// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NamedArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamedArgument");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.ext.csharp.syntax.AttributeArgumentExpression value;
  
  public NamedArgument (hydra.ext.csharp.syntax.Identifier name, hydra.ext.csharp.syntax.AttributeArgumentExpression value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedArgument)) {
      return false;
    }
    NamedArgument o = (NamedArgument) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public NamedArgument withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new NamedArgument(name, value);
  }
  
  public NamedArgument withValue(hydra.ext.csharp.syntax.AttributeArgumentExpression value) {
    java.util.Objects.requireNonNull((value));
    return new NamedArgument(name, value);
  }
}