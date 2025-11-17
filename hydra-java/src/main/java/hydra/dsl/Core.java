package hydra.dsl;

import hydra.core.Name;


/**
 * Core DSL utilities for working with basic Hydra constructs.
 */
public interface Core {
  /**
   * Create a field name from a string.
   *
   * @param name the string name
   * @return a Name instance representing the field name
   */
  static Name fieldName(final String name) {
    return new Name(name);
  }

  /**
   * Create a Name from a string.
   *
   * @param name the string name
   * @return a Name instance
   */
  static Name name(final String name) {
    return new Name(name);
  }
}
