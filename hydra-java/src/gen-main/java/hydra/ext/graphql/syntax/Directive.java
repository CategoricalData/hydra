package hydra.ext.graphql.syntax;

public class Directive {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.Directive");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Arguments> arguments;
  
  public Directive (hydra.ext.graphql.syntax.Name name, java.util.Optional<hydra.ext.graphql.syntax.Arguments> arguments) {
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
    return new Directive(name, arguments);
  }
  
  public Directive withArguments(java.util.Optional<hydra.ext.graphql.syntax.Arguments> arguments) {
    return new Directive(name, arguments);
  }
}