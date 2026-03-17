// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type expression together with free type variables occurring in the expression
 */
public class TypeScheme implements Serializable, Comparable<TypeScheme> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.core.TypeScheme");

  public static final hydra.core.Name VARIABLES = new hydra.core.Name("variables");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name CONSTRAINTS = new hydra.core.Name("constraints");

  /**
   * The free type variables
   */
  public final hydra.util.ConsList<hydra.core.Name> variables;

  /**
   * The type expression
   */
  public final hydra.core.Type type;

  /**
   * Optional metadata for type variables, including typeclass constraints. The map keys are type variable names.
   */
  public final hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints;

  public TypeScheme (hydra.util.ConsList<hydra.core.Name> variables, hydra.core.Type type, hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints) {
    this.variables = variables;
    this.type = type;
    this.constraints = constraints;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeScheme)) {
      return false;
    }
    TypeScheme o = (TypeScheme) other;
    return java.util.Objects.equals(
      this.variables,
      o.variables) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.constraints,
      o.constraints);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variables) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(constraints);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeScheme other) {
    int cmp = 0;
    cmp = ((Comparable) variables).compareTo(other.variables);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) constraints).compareTo(other.constraints);
  }

  public TypeScheme withVariables(hydra.util.ConsList<hydra.core.Name> variables) {
    return new TypeScheme(variables, type, constraints);
  }

  public TypeScheme withType(hydra.core.Type type) {
    return new TypeScheme(variables, type, constraints);
  }

  public TypeScheme withConstraints(hydra.util.Maybe<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>> constraints) {
    return new TypeScheme(variables, type, constraints);
  }
}
