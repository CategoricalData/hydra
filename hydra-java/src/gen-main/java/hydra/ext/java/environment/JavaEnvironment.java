// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.environment;

import java.io.Serializable;

/**
 * Environment for Java code generation
 */
public class JavaEnvironment implements Serializable, Comparable<JavaEnvironment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.environment.JavaEnvironment");

  public static final hydra.core.Name ALIASES = new hydra.core.Name("aliases");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  /**
   * Aliases and context state
   */
  public final hydra.ext.java.environment.Aliases aliases;

  /**
   * Graph context for type inference
   */
  public final hydra.graph.Graph graph;

  public JavaEnvironment (hydra.ext.java.environment.Aliases aliases, hydra.graph.Graph graph) {
    this.aliases = aliases;
    this.graph = graph;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JavaEnvironment)) {
      return false;
    }
    JavaEnvironment o = (JavaEnvironment) other;
    return java.util.Objects.equals(
      this.aliases,
      o.aliases) && java.util.Objects.equals(
      this.graph,
      o.graph);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(aliases) + 3 * java.util.Objects.hashCode(graph);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(JavaEnvironment other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      aliases,
      other.aliases);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      graph,
      other.graph);
  }

  public JavaEnvironment withAliases(hydra.ext.java.environment.Aliases aliases) {
    return new JavaEnvironment(aliases, graph);
  }

  public JavaEnvironment withGraph(hydra.graph.Graph graph) {
    return new JavaEnvironment(aliases, graph);
  }
}
