// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class Directive implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.Directive");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Arguments> arguments;
  
  public Directive (hydra.ext.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.graphql.syntax.Arguments> arguments) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((arguments));
    this.name = name;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Directive)) {
      return false;
    }
    Directive o = (Directive) (other);
    return name.equals(o.name) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * arguments.hashCode();
  }
  
  public Directive withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Directive(name, arguments);
  }
  
  public Directive withArguments(hydra.util.Opt<hydra.ext.graphql.syntax.Arguments> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new Directive(name, arguments);
  }
}
