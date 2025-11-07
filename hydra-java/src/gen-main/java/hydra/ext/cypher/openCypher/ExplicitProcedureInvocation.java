// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ExplicitProcedureInvocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.ExplicitProcedureInvocation");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.cypher.openCypher.QualifiedName name;
  
  public final java.util.List<hydra.ext.cypher.openCypher.Expression> arguments;
  
  public ExplicitProcedureInvocation (hydra.ext.cypher.openCypher.QualifiedName name, java.util.List<hydra.ext.cypher.openCypher.Expression> arguments) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((arguments));
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
  
  public ExplicitProcedureInvocation withName(hydra.ext.cypher.openCypher.QualifiedName name) {
    java.util.Objects.requireNonNull((name));
    return new ExplicitProcedureInvocation(name, arguments);
  }
  
  public ExplicitProcedureInvocation withArguments(java.util.List<hydra.ext.cypher.openCypher.Expression> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ExplicitProcedureInvocation(name, arguments);
  }
}
