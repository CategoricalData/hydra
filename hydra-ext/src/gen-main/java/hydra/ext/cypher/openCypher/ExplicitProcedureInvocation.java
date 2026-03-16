// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class ExplicitProcedureInvocation implements Serializable, Comparable<ExplicitProcedureInvocation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ExplicitProcedureInvocation");
  
  public static final hydra.core.Name NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.cypher.openCypher.QualifiedName name;
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.Expression> arguments;
  
  public ExplicitProcedureInvocation (hydra.ext.cypher.openCypher.QualifiedName name, hydra.util.ConsList<hydra.ext.cypher.openCypher.Expression> arguments) {
    this.name = name;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExplicitProcedureInvocation)) {
      return false;
    }
    ExplicitProcedureInvocation o = (ExplicitProcedureInvocation) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(arguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExplicitProcedureInvocation other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) arguments).compareTo(other.arguments);
  }
  
  public ExplicitProcedureInvocation withName(hydra.ext.cypher.openCypher.QualifiedName name) {
    return new ExplicitProcedureInvocation(name, arguments);
  }
  
  public ExplicitProcedureInvocation withArguments(hydra.util.ConsList<hydra.ext.cypher.openCypher.Expression> arguments) {
    return new ExplicitProcedureInvocation(name, arguments);
  }
}
