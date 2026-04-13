// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class FunctionInvocation implements Serializable, Comparable<FunctionInvocation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.FunctionInvocation");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name DISTINCT = new hydra.core.Name("distinct");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  public final hydra.cypher.openCypher.QualifiedName name;

  public final Boolean distinct;

  public final java.util.List<hydra.cypher.openCypher.Expression> arguments;

  public FunctionInvocation (hydra.cypher.openCypher.QualifiedName name, Boolean distinct, java.util.List<hydra.cypher.openCypher.Expression> arguments) {
    this.name = name;
    this.distinct = distinct;
    this.arguments = arguments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionInvocation)) {
      return false;
    }
    FunctionInvocation o = (FunctionInvocation) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.distinct,
      o.distinct) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(distinct) + 5 * java.util.Objects.hashCode(arguments);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionInvocation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      distinct,
      other.distinct);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      arguments,
      other.arguments);
  }

  public FunctionInvocation withName(hydra.cypher.openCypher.QualifiedName name) {
    return new FunctionInvocation(name, distinct, arguments);
  }

  public FunctionInvocation withDistinct(Boolean distinct) {
    return new FunctionInvocation(name, distinct, arguments);
  }

  public FunctionInvocation withArguments(java.util.List<hydra.cypher.openCypher.Expression> arguments) {
    return new FunctionInvocation(name, distinct, arguments);
  }
}
