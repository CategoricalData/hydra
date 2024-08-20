// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class FunctionInvocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.FunctionInvocation");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DISTINCT = new hydra.core.Name("distinct");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.cypher.openCypher.QualifiedName name;
  
  public final Boolean distinct;
  
  public final java.util.List<hydra.ext.cypher.openCypher.Expression> arguments;
  
  public FunctionInvocation (hydra.ext.cypher.openCypher.QualifiedName name, Boolean distinct, java.util.List<hydra.ext.cypher.openCypher.Expression> arguments) {
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
  
  public FunctionInvocation withName(hydra.ext.cypher.openCypher.QualifiedName name) {
    java.util.Objects.requireNonNull((name));
    return new FunctionInvocation(name, distinct, arguments);
  }
  
  public FunctionInvocation withDistinct(Boolean distinct) {
    java.util.Objects.requireNonNull((distinct));
    return new FunctionInvocation(name, distinct, arguments);
  }
  
  public FunctionInvocation withArguments(java.util.List<hydra.ext.cypher.openCypher.Expression> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new FunctionInvocation(name, distinct, arguments);
  }
}
