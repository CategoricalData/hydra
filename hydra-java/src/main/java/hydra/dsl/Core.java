package hydra.impl.java.dsl;

import hydra.core.FieldName;
import hydra.core.Name;


public interface Core {
  static FieldName fieldName(final String name) {
    return new FieldName(name);
  }

  static Name name(final String name) {
    return new Name(name);
  }
}
