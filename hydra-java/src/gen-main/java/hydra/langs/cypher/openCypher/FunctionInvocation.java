// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class FunctionInvocation implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.FunctionInvocation");
  
  public final hydra.langs.cypher.openCypher.QualifiedName name;
  
  public final Boolean distinct;
  
  public final java.util.List<hydra.langs.cypher.openCypher.Expression> arguments;
  
  public FunctionInvocation (hydra.langs.cypher.openCypher.QualifiedName name, Boolean distinct, java.util.List<hydra.langs.cypher.openCypher.Expression> arguments) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((distinct));
    java.util.Objects.requireNonNull((arguments));
    this.name = name;
    this.distinct = distinct;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionInvocation)) {
      return false;
    }
    FunctionInvocation o = (FunctionInvocation) (other);
    return name.equals(o.name) && distinct.equals(o.distinct) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * distinct.hashCode() + 5 * arguments.hashCode();
  }
  
  public FunctionInvocation withName(hydra.langs.cypher.openCypher.QualifiedName name) {
    java.util.Objects.requireNonNull((name));
    return new FunctionInvocation(name, distinct, arguments);
  }
  
  public FunctionInvocation withDistinct(Boolean distinct) {
    java.util.Objects.requireNonNull((distinct));
    return new FunctionInvocation(name, distinct, arguments);
  }
  
  public FunctionInvocation withArguments(java.util.List<hydra.langs.cypher.openCypher.Expression> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new FunctionInvocation(name, distinct, arguments);
  }
}