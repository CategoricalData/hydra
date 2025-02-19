// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class Attribute implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Attribute");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.csharp.syntax.AttributeName name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.AttributeArguments> arguments;
  
  public Attribute (hydra.ext.csharp.syntax.AttributeName name, hydra.util.Opt<hydra.ext.csharp.syntax.AttributeArguments> arguments) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((arguments));
    this.name = name;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Attribute)) {
      return false;
    }
    Attribute o = (Attribute) (other);
    return name.equals(o.name) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * arguments.hashCode();
  }
  
  public Attribute withName(hydra.ext.csharp.syntax.AttributeName name) {
    java.util.Objects.requireNonNull((name));
    return new Attribute(name, arguments);
  }
  
  public Attribute withArguments(hydra.util.Opt<hydra.ext.csharp.syntax.AttributeArguments> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new Attribute(name, arguments);
  }
}