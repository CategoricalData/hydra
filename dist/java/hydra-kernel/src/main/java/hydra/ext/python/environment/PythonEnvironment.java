// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.environment;

import java.io.Serializable;

/**
 * Environment for Python code generation
 */
public class PythonEnvironment implements Serializable, Comparable<PythonEnvironment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.environment.PythonEnvironment");

  public static final hydra.core.Name NAMESPACES = new hydra.core.Name("namespaces");

  public static final hydra.core.Name BOUND_TYPE_VARIABLES = new hydra.core.Name("boundTypeVariables");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name NULLARY_BINDINGS = new hydra.core.Name("nullaryBindings");

  public static final hydra.core.Name VERSION = new hydra.core.Name("version");

  public static final hydra.core.Name SKIP_CASTS = new hydra.core.Name("skipCasts");

  public static final hydra.core.Name INLINE_VARIABLES = new hydra.core.Name("inlineVariables");

  /**
   * Namespace mapping for imports
   */
  public final hydra.packaging.Namespaces<hydra.ext.python.syntax.DottedName> namespaces;

  /**
   * Type variables in scope, with their Python names
   */
  public final hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundTypeVariables;

  /**
   * Graph context for type inference
   */
  public final hydra.graph.Graph graph;

  /**
   * Set of nullary bindings (need call syntax)
   */
  public final java.util.Set<hydra.core.Name> nullaryBindings;

  /**
   * Target Python version
   */
  public final hydra.ext.python.environment.PythonVersion version;

  /**
   * When True, skip generating cast() calls for reduced memory usage
   */
  public final Boolean skipCasts;

  /**
   * Variables that are inline let bindings (walrus operators)
   */
  public final java.util.Set<hydra.core.Name> inlineVariables;

  public PythonEnvironment (hydra.packaging.Namespaces<hydra.ext.python.syntax.DottedName> namespaces, hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundTypeVariables, hydra.graph.Graph graph, java.util.Set<hydra.core.Name> nullaryBindings, hydra.ext.python.environment.PythonVersion version, Boolean skipCasts, java.util.Set<hydra.core.Name> inlineVariables) {
    this.namespaces = namespaces;
    this.boundTypeVariables = boundTypeVariables;
    this.graph = graph;
    this.nullaryBindings = nullaryBindings;
    this.version = version;
    this.skipCasts = skipCasts;
    this.inlineVariables = inlineVariables;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PythonEnvironment)) {
      return false;
    }
    PythonEnvironment o = (PythonEnvironment) other;
    return java.util.Objects.equals(
      this.namespaces,
      o.namespaces) && java.util.Objects.equals(
      this.boundTypeVariables,
      o.boundTypeVariables) && java.util.Objects.equals(
      this.graph,
      o.graph) && java.util.Objects.equals(
      this.nullaryBindings,
      o.nullaryBindings) && java.util.Objects.equals(
      this.version,
      o.version) && java.util.Objects.equals(
      this.skipCasts,
      o.skipCasts) && java.util.Objects.equals(
      this.inlineVariables,
      o.inlineVariables);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespaces) + 3 * java.util.Objects.hashCode(boundTypeVariables) + 5 * java.util.Objects.hashCode(graph) + 7 * java.util.Objects.hashCode(nullaryBindings) + 11 * java.util.Objects.hashCode(version) + 13 * java.util.Objects.hashCode(skipCasts) + 17 * java.util.Objects.hashCode(inlineVariables);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PythonEnvironment other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      namespaces,
      other.namespaces);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      boundTypeVariables,
      other.boundTypeVariables);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      graph,
      other.graph);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      nullaryBindings,
      other.nullaryBindings);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      version,
      other.version);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      skipCasts,
      other.skipCasts);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      inlineVariables,
      other.inlineVariables);
  }

  public PythonEnvironment withNamespaces(hydra.packaging.Namespaces<hydra.ext.python.syntax.DottedName> namespaces) {
    return new PythonEnvironment(namespaces, boundTypeVariables, graph, nullaryBindings, version, skipCasts, inlineVariables);
  }

  public PythonEnvironment withBoundTypeVariables(hydra.util.Pair<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundTypeVariables) {
    return new PythonEnvironment(namespaces, boundTypeVariables, graph, nullaryBindings, version, skipCasts, inlineVariables);
  }

  public PythonEnvironment withGraph(hydra.graph.Graph graph) {
    return new PythonEnvironment(namespaces, boundTypeVariables, graph, nullaryBindings, version, skipCasts, inlineVariables);
  }

  public PythonEnvironment withNullaryBindings(java.util.Set<hydra.core.Name> nullaryBindings) {
    return new PythonEnvironment(namespaces, boundTypeVariables, graph, nullaryBindings, version, skipCasts, inlineVariables);
  }

  public PythonEnvironment withVersion(hydra.ext.python.environment.PythonVersion version) {
    return new PythonEnvironment(namespaces, boundTypeVariables, graph, nullaryBindings, version, skipCasts, inlineVariables);
  }

  public PythonEnvironment withSkipCasts(Boolean skipCasts) {
    return new PythonEnvironment(namespaces, boundTypeVariables, graph, nullaryBindings, version, skipCasts, inlineVariables);
  }

  public PythonEnvironment withInlineVariables(java.util.Set<hydra.core.Name> inlineVariables) {
    return new PythonEnvironment(namespaces, boundTypeVariables, graph, nullaryBindings, version, skipCasts, inlineVariables);
  }
}
