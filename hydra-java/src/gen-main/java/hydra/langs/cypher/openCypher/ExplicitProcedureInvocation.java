// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ExplicitProcedureInvocation implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ExplicitProcedureInvocation");
  
  public final hydra.langs.cypher.openCypher.QualifiedName name;
  
  public final java.util.List<hydra.langs.cypher.openCypher.Expression> arguments;
  
  public ExplicitProcedureInvocation (hydra.langs.cypher.openCypher.QualifiedName name, java.util.List<hydra.langs.cypher.openCypher.Expression> arguments) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    this.name = name;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExplicitProcedureInvocation)) {
      return false;
    }
    ExplicitProcedureInvocation o = (ExplicitProcedureInvocation) (other);
    return name.equals(o.name) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * arguments.hashCode();
  }
  
  public ExplicitProcedureInvocation withName(hydra.langs.cypher.openCypher.QualifiedName name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new ExplicitProcedureInvocation(name, arguments);
  }
  
  public ExplicitProcedureInvocation withArguments(java.util.List<hydra.langs.cypher.openCypher.Expression> arguments) {
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    return new ExplicitProcedureInvocation(name, arguments);
  }
}