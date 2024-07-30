package hydra.dsl;

import hydra.core.Name;


public interface Core {
  static Name fieldName(final String name) {
    return new Name(name);
  }

  static Name name(final String name) {
    return new Name(name);
  }
}
