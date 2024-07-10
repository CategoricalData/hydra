// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class FunctionInvocation implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.FunctionInvocation");
  
  public final hydra.langs.cypher.openCypher.QualifiedName name;
  
  public final Boolean distinct;
  
  public final java.util.List<hydra.langs.cypher.openCypher.Expression> arguments;
  
  public FunctionInvocation (hydra.langs.cypher.openCypher.QualifiedName name, Boolean distinct, java.util.List<hydra.langs.cypher.openCypher.Expression> arguments) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (distinct == null) {
      throw new IllegalArgumentException("null value for 'distinct' argument");
    }
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
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
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new FunctionInvocation(name, distinct, arguments);
  }
  
  public FunctionInvocation withDistinct(Boolean distinct) {
    if (distinct == null) {
      throw new IllegalArgumentException("null value for 'distinct' argument");
    }
    return new FunctionInvocation(name, distinct, arguments);
  }
  
  public FunctionInvocation withArguments(java.util.List<hydra.langs.cypher.openCypher.Expression> arguments) {
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    return new FunctionInvocation(name, distinct, arguments);
  }
}