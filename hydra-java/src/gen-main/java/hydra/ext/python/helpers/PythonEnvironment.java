// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.helpers;

import java.io.Serializable;

/**
 * Environment for Python code generation
 */
public class PythonEnvironment implements Serializable, Comparable<PythonEnvironment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.helpers.PythonEnvironment");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACES = new hydra.core.Name("namespaces");
  
  public static final hydra.core.Name FIELD_NAME_BOUND_TYPE_VARIABLES = new hydra.core.Name("boundTypeVariables");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CONTEXT = new hydra.core.Name("typeContext");
  
  public static final hydra.core.Name FIELD_NAME_NULLARY_BINDINGS = new hydra.core.Name("nullaryBindings");
  
  public static final hydra.core.Name FIELD_NAME_VERSION = new hydra.core.Name("version");
  
  public static final hydra.core.Name FIELD_NAME_SKIP_CASTS = new hydra.core.Name("skipCasts");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_VARIABLES = new hydra.core.Name("inlineVariables");
  
  /**
   * Namespace mapping for imports
   */
  public final hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces;
  
  /**
   * Type variables in scope, with their Python names
   */
  public final hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundTypeVariables;
  
  /**
   * Type context for type inference
   */
  public final hydra.typing.TypeContext typeContext;
  
  /**
   * Set of nullary bindings (need call syntax)
   */
  public final java.util.Set<hydra.core.Name> nullaryBindings;
  
  /**
   * Target Python version
   */
  public final hydra.ext.python.helpers.PythonVersion version;
  
  /**
   * When True, skip generating cast() calls for reduced memory usage
   */
  public final Boolean skipCasts;
  
  /**
   * Variables that are inline let bindings (walrus operators)
   */
  public final java.util.Set<hydra.core.Name> inlineVariables;
  
  public PythonEnvironment (hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces, hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundTypeVariables, hydra.typing.TypeContext typeContext, java.util.Set<hydra.core.Name> nullaryBindings, hydra.ext.python.helpers.PythonVersion version, Boolean skipCasts, java.util.Set<hydra.core.Name> inlineVariables) {
    this.namespaces = namespaces;
    this.boundTypeVariables = boundTypeVariables;
    this.typeContext = typeContext;
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
      this.typeContext,
      o.typeContext) && java.util.Objects.equals(
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
    return 2 * java.util.Objects.hashCode(namespaces) + 3 * java.util.Objects.hashCode(boundTypeVariables) + 5 * java.util.Objects.hashCode(typeContext) + 7 * java.util.Objects.hashCode(nullaryBindings) + 11 * java.util.Objects.hashCode(version) + 13 * java.util.Objects.hashCode(skipCasts) + 17 * java.util.Objects.hashCode(inlineVariables);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PythonEnvironment other) {
    int cmp = 0;
    cmp = ((Comparable) namespaces).compareTo(other.namespaces);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      boundTypeVariables.hashCode(),
      other.boundTypeVariables.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) typeContext).compareTo(other.typeContext);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      nullaryBindings.hashCode(),
      other.nullaryBindings.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) version).compareTo(other.version);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) skipCasts).compareTo(other.skipCasts);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      inlineVariables.hashCode(),
      other.inlineVariables.hashCode());
  }
  
  public PythonEnvironment withNamespaces(hydra.module.Namespaces<hydra.ext.python.syntax.DottedName> namespaces) {
    return new PythonEnvironment(namespaces, boundTypeVariables, typeContext, nullaryBindings, version, skipCasts, inlineVariables);
  }
  
  public PythonEnvironment withBoundTypeVariables(hydra.util.Tuple.Tuple2<java.util.List<hydra.core.Name>, java.util.Map<hydra.core.Name, hydra.ext.python.syntax.Name>> boundTypeVariables) {
    return new PythonEnvironment(namespaces, boundTypeVariables, typeContext, nullaryBindings, version, skipCasts, inlineVariables);
  }
  
  public PythonEnvironment withTypeContext(hydra.typing.TypeContext typeContext) {
    return new PythonEnvironment(namespaces, boundTypeVariables, typeContext, nullaryBindings, version, skipCasts, inlineVariables);
  }
  
  public PythonEnvironment withNullaryBindings(java.util.Set<hydra.core.Name> nullaryBindings) {
    return new PythonEnvironment(namespaces, boundTypeVariables, typeContext, nullaryBindings, version, skipCasts, inlineVariables);
  }
  
  public PythonEnvironment withVersion(hydra.ext.python.helpers.PythonVersion version) {
    return new PythonEnvironment(namespaces, boundTypeVariables, typeContext, nullaryBindings, version, skipCasts, inlineVariables);
  }
  
  public PythonEnvironment withSkipCasts(Boolean skipCasts) {
    return new PythonEnvironment(namespaces, boundTypeVariables, typeContext, nullaryBindings, version, skipCasts, inlineVariables);
  }
  
  public PythonEnvironment withInlineVariables(java.util.Set<hydra.core.Name> inlineVariables) {
    return new PythonEnvironment(namespaces, boundTypeVariables, typeContext, nullaryBindings, version, skipCasts, inlineVariables);
  }
}
